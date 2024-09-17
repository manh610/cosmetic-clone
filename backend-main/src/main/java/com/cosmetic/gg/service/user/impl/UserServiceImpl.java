package com.cosmetic.gg.service.user.impl;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.authentication.jwt.JwtUtil;
import com.cosmetic.gg.authentication.service.UserDetailsServiceImpl;
import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.email.EmailSender;
import com.cosmetic.gg.common.email.Message;
import com.cosmetic.gg.common.enums.EGender;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.model.UserModel;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.discount.DiscountRepository;
import com.cosmetic.gg.service.user.UserService;

@Service
public class UserServiceImpl implements UserService{
	
	private static final Logger log = LoggerFactory.getLogger(UserServiceImpl.class);
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private DiscountRepository discountRepository;
	
	@Autowired
    private UserDetailsServiceImpl userDetailsServiceImpl;
	
	@Autowired
    private PasswordEncoder bcryptEncoder;
	
	@Autowired
	private EmailSender mailSender;
	
	@Autowired
	private JwtUtil jwtUtil;
	
	@Override
	public Map<String, Object> search(String keyword, EStatus status, String roleId,
			EUserRank userRank, Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			
			List<User> users = userRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(status) ? null : status.name(),
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(roleId)) ? null : roleId,
					Objects.isNull(userRank) ? null : userRank.name(),
					pageIndex, pageSize);
			Integer totalItem = userRepository.cntUser(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(status) ? null : status.name(),
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(roleId)) ? null : roleId,
					Objects.isNull(userRank) ? null : userRank.name());
			
			result.put("data", users);
			result.put("totalItem", totalItem);
			return result;
		}catch(Exception ex) {
			result.put("data", new ArrayList<User>());
			result.put("totalItem", 0);
			log.error("Error while searching user", ex.getCause());
		}
		return result;
	}

	public User getById(String id) {
		try {
			return userRepository.findById(id).orElse(null);
		}catch(Exception ex) {
			log.error(String.format("Error while getting user by id: %s", id), ex.getCause());
			return null;
		}
	}
	
	@Override
	  public UserModel getByKey(String key) {
	    if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(key)))
	      return null;

	    try {
	      User userEntity = userRepository.findByKey(key);
	      return ModelMapper.map(userEntity, UserModel.class);
	    } catch (Exception ex) {
	      log.error(String.format("Error while getting user by key: %s", key), ex.getCause());
	      return null;
	    }
	  }
	
	@Override
	public List<Error> validator(UserModel userModel) {
		List<Error> errors = new ArrayList<>();
		try {
			User userCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(userModel.getId()))) {
				userCheck = userRepository.findById(userModel.getId()).orElse(null);
				if(userCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert userCheck != null;
				if(userCheck.getStatus() == EStatus.DELETED ||
						userCheck.getStatus() == EStatus.INACTIVE ||
						userCheck.getStatus() == EStatus.DRAFT)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
			userCheck = userRepository.findByKey(userModel.getUsername());
			if(userCheck != null && !userCheck.getId().equals(userModel.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_USERNAME));
			
			userCheck = userRepository.findByKey(userModel.getEmail());
			if(userCheck != null && !userCheck.getId().equals(userModel.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_EMAIL));
			
			userCheck = userRepository.findByKey(userModel.getPhone());
			if(userCheck != null && !userCheck.getId().equals(userModel.getId()))
				errors.add(new Error().builder(ErrorCode.USER_EXIST_PHONE));
			
			if(!StringUtils.isNullOrEmpty(userModel.getCitizenNumber())) {
				userCheck = userRepository.findByKey(userModel.getCitizenNumber());
				if(userCheck != null && !userCheck.getId().equals(userModel.getId()))
					errors.add(new Error().builder(ErrorCode.USER_EXIST_CITIZEN_NUMBER));
			}
			
		}catch(Exception ex) {
			log.error("Error while validating user data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public UserModel create(UserModel userModel) {
		try {
			User userEntity = ModelMapper.map(userModel, User.class);
			userEntity.prepareEntity();
			userEntity.setPassword(bcryptEncoder.encode(userModel.getPassword()));
			UserModel result = ModelMapper.map(userRepository.save(userEntity), UserModel.class);
			if (result == null)
				return null;
			return result;
		}catch(Exception ex) {
			log.error("Error while creating user", ex.getCause());
			return null;
		}
	}

	@Override
	public UserModel update(UserModel userModel) {
		try {
			User entity = ModelMapper.map(userModel, User.class);
			if (entity == null)
				return null;
			
			User userCheck = userRepository.findById(entity.getId()).orElse(null);
			entity.prepareEntity(userModel.getStatus());
			entity.setPassword(userCheck.getPassword());
			UserModel result = ModelMapper.map(userRepository.save(entity), UserModel.class);
			if (result == null)
				return null;
			return result;
		}catch(Exception ex) {
			log.error("Error while updating user", ex.getCause());
			return null;
		}
	}
	
	@Override
	public ErrorCode changeImage(MultipartFile imageFile, String id) {
		try {
			User userEntity = getById(id);
			if (userEntity == null)
		        return ErrorCode.NOT_FOUND;
			
			if (userEntity.getStatus() == EStatus.DELETED)
		    	return ErrorCode.INVALID_STATUS;
			
			String originalFilename = imageFile.getOriginalFilename();
	        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
	        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
	        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
	        			&& !extension.equals("svg") && !extension.equals("jpeg"))
	        		return ErrorCode.INVALID_IMAGE;
	        }
			
			byte[] img = imageFile.getBytes();
			if(img == null) {
				log.error("Error while convert image");
				return ErrorCode.ERROR_CONVERT_IMAGE;
			}
			
			userEntity.setAvatar(img);
			userEntity = userRepository.save(userEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(userEntity.getId())))
				return ErrorCode.FAILURE;
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while changing image user", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			User userEntity = getById(id);
			if (userEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			if (userEntity.getStatus() == EStatus.DRAFT) {
				userRepository.deleteById(id);
				return new Error().builder(ErrorCode.SUCCESS);
			}
			
			userEntity.setStatus(EStatus.DELETED);
			User result = userRepository.save(userEntity);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting user", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}

	@Override
	public UserModel detail(String id) {
		try {
			Object rs = new Object();
			User userEntity = userRepository.findById(id).orElse(null);
			if(userEntity == null) return null;
			if(StringUtils.isNullOrEmpty(userEntity.getDeliveryUnitId())) rs = userRepository.detailV2(id);
			else rs = userRepository.detailV1(id);
			if(rs instanceof Object[]) {
				Object[] objArray = (Object[]) rs;
				UserModel userModel = new UserModel();
				userModel.setId((String) objArray[0]);
				userModel.setUsername((String) objArray[1]);
				userModel.setEmail((String) objArray[2]);
				userModel.setPhone((String) objArray[3]);
				userModel.setCitizenNumber((String) objArray[4]);
				userModel.setGivenName((String) objArray[5]);
				userModel.setFamilyName((String) objArray[6]);
				
				EGender gender = ((String) objArray[7]).equalsIgnoreCase("MALE")? EGender.MALE : EGender.FEMALE;
				userModel.setGender(gender);
				
				userModel.setDob(((java.sql.Timestamp) objArray[8]).toLocalDateTime());
				userModel.setCountry((String) objArray[9]);
				userModel.setPassword((String) objArray[10]);
				
				EUserRank userRank = null;
				if(((String) objArray[11]).equalsIgnoreCase("PRIORITY")) userRank = EUserRank.PRIORITY;
				else if (((String) objArray[11]).equalsIgnoreCase("SILVER")) userRank = EUserRank.SILVER;
				else if (((String) objArray[11]).equalsIgnoreCase("GOLD")) userRank = EUserRank.GOLD;
				else if (((String) objArray[11]).equalsIgnoreCase("DIAMONDS")) userRank = EUserRank.DIAMONDS;
				else userRank = EUserRank.MEMBER;
				userModel.setUserRank(userRank);
				
				userModel.setAvatar((byte[]) objArray[12]);
				userModel.setRoleId((String) objArray[13]);
				userModel.setDeliveryUnitId((String) objArray[14]);
				
				EStatus status = null;
				if(((String) objArray[15]).equalsIgnoreCase("INACTIVE")) status = EStatus.INACTIVE;
				else if(((String) objArray[15]).equalsIgnoreCase("DRAFT")) status = EStatus.DRAFT;
				else status = EStatus.ACTIVE;
				userModel.setStatus(status);
				
				userModel.setDescription((String) objArray[16]);
				userModel.setRoleName((String) objArray[17]);
				if(!StringUtils.isNullOrEmpty(userEntity.getDeliveryUnitId()))
					userModel.setDeliveryUnitName((String) objArray[18]);
				
				return userModel;
			}
			return null;
		}catch(Exception ex) {
			log.error(String.format("Error while getting detail user by id: %s", id), ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error blockAccount(String id) {
		try {
			User userEntity = getById(id);
			if (userEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			if(userEntity.getStatus() == EStatus.ACTIVE) userEntity.setStatus(EStatus.BLOCK);
			else if (userEntity.getStatus() == EStatus.BLOCK) userEntity.setStatus(EStatus.ACTIVE);
			else return new Error().builder(ErrorCode.INVALID_STATUS);
						
			User result = userRepository.save(userEntity);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error(String.format("Error while blocking user by id: %s", id), ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public Error recoverAccount(String id) {
		try {
			User userEntity = getById(id);
			if (userEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			if (userEntity.getStatus() != EStatus.DELETED) 
				return new Error().builder(ErrorCode.INVALID_STATUS);
			
			userEntity.setStatus(EStatus.ACTIVE);
			User result = userRepository.save(userEntity);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error(String.format("Error while recovering user by id: %s", id), ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public UserModel register(UserModel userModel) {
		String userId = "";
		try {
			User userEntity = ModelMapper.map(userModel, User.class);
			if (userEntity == null)
		        return null;
			  String password = StringUtils.generateRandomString();
		      userEntity.setPassword(bcryptEncoder.encode(password));
		      userEntity.prepareEntity();
		      UserModel result = ModelMapper.map(userRepository.save(userEntity), UserModel.class);
		      
		      mailSender.sendMail(result.getEmail(), 
    	    		  Message.ACTIVE_ACCOUNT, 
    	    		  Message.DEAR_ACTIVE + result.getUsername() + ". " + Message.PASSWORD + password);
		      
		      return result;
		}catch(Exception ex) {
			log.error("Error while register user", ex.getMessage());
			userRepository.deleteById(userId);
			return null;
		}
	}
	
	@Override
	public Error changePassword(String oldPassword, String newPassword, String username) {
		try {
			User currentUser = userRepository.findByKey(username);
			if(currentUser == null)
				return new Error().builder(
			            ErrorCode.NOT_FOUND);
			if(currentUser.getStatus() != EStatus.ACTIVE)
				return new Error().builder(
			            ErrorCode.INVALID_STATUS);
			Error error = userDetailsServiceImpl.authenticate(currentUser.getUsername(), oldPassword);
			if (!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode())) {
				return new Error().builder(
			            ErrorCode.INVALID_DATA,
			            "Mật khẩu cũ không hợp lệ",
			            "Old password is invalid");
			}
			
			currentUser.setPassword(bcryptEncoder.encode(newPassword));
			userRepository.save(currentUser);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while changing password of current user", ex.getCause());
		      return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public User getUserFromToken(String accessToken) {
		User userEntity = new User();
		try {
			String username = jwtUtil.getUsernameFromToken(accessToken);
			userEntity = userRepository.findByKey(username);
			if(userEntity == null || userEntity.getStatus() != EStatus.ACTIVE)
				return null;
			return userEntity;
		}catch (Exception e) {
	      log.error(e.getMessage());
	      throw e;
	    }
	}
	
	@Override
	public List<Discount> getDiscountByUser(String id, Boolean use) {
		List<Discount> result = new ArrayList<>();
		try {
			User userEntity = getById(id);
			if (userEntity == null || userEntity.getStatus() != EStatus.ACTIVE)
				return null;
			
			result = discountRepository.getDiscountByUser(id,
					Objects.isNull(use) ? null : use);
			return result;
		}catch(Exception ex) {
			log.error("Error while getting discount by user", ex.getCause());
		      return null;
		}
	}
	
	@Override
	public LocalDateTime getExpireTimeToken(String accessToken) {
		try {
			Date expireTimeDate = jwtUtil.getExpirationDateFromToken(accessToken);
			LocalDateTime expireTime = dateToLocalDateTime(expireTimeDate);
			return expireTime;
		}catch (Exception e) {
	      log.error(e.getMessage());
	      throw e;
	    }
	}
	
	private LocalDateTime dateToLocalDateTime(Date date) {
        // Convert Date to Instant
        Instant instant = date.toInstant();

        // Convert Instant to LocalDateTime
        LocalDateTime localDateTime = instant.atZone(ZoneId.systemDefault()).toLocalDateTime();
        return localDateTime;
    }
}
