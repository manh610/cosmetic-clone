package com.cosmetic.gg.service.product.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.dto.response.product.ProductResponse;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.product.Cart;
import com.cosmetic.gg.entity.product.Favorite;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.product.FavoriteRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.product.FavoriteService;

@Service
public class FavoriteServiceImpl implements FavoriteService{

	private static final Logger log = LoggerFactory.getLogger(FavoriteServiceImpl.class);
	
	@Autowired
	private FavoriteRepository favoriteRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private UserRepository userRepository;
	
	@Override
	public List<ProductResponse> search(String id) {
		List<ProductResponse> result = new ArrayList<>();
		try {
			User userCheck = userRepository.findById(id).orElse(null);
			if(userCheck == null || userCheck.getStatus() != EStatus.ACTIVE)
				return result;
			
			List<Object> favorites = productRepository.getProductFavoriteByUser(userCheck.getId());
			for(Object rs: favorites) {
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ProductResponse productResponse = new ProductResponse();
					productResponse.setId((String) objArray[0]);
					productResponse.setName((String) objArray[2]);
					productResponse.setCode((String) objArray[1]);
					productResponse.setPhoto((byte[]) objArray[3]);
					productResponse.setMadeIn((String) objArray[4]);
					
					EStatus statuss = null;
					if(((String) objArray[5]).equalsIgnoreCase("STOCK")) statuss = EStatus.STOCK;
					else if (((String) objArray[5]).equalsIgnoreCase("SOLD_OUT")) statuss = EStatus.SOLD_OUT;
					else statuss = EStatus.HIDDEN;
					productResponse.setStatus(statuss);
					productResponse.setDescription((String) objArray[6]);
					productResponse.setProductionDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
					productResponse.setExpirationDate(((java.sql.Timestamp) objArray[8]).toLocalDateTime());
					productResponse.setCategoryId((String) objArray[9]);
					productResponse.setBrandId((String) objArray[10]);
					productResponse.setCategoryName((String) objArray[11]);
					productResponse.setBrandName((String) objArray[12]);
					productResponse.setMinPrice((Float) objArray[13]);
					productResponse.setMaxPrice((Float) objArray[14]);
					
					List<String> skinTypePairs = Arrays.asList(((String) objArray[15]).split(","));
					List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
					for(String pair: skinTypePairs) {
						String[] parts = pair.split(":");
						if(parts.length == 2) {
							SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
							skinTypeResponse.setId(parts[0]);
							skinTypeResponse.setName(parts[1]);
							skinTypeResponses.add(skinTypeResponse);
						}
					}
					productResponse.setSkinTypes(skinTypeResponses);
					Integer totalFavorite = favoriteRepository.cntProductFavorite((String) objArray[0]);
					productResponse.setTotalFavorite(totalFavorite);
					result.add(productResponse);
				}
			}
			return result;
		}catch(Exception ex) {
			log.error("Error while searching product to favorite", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public List<Error> validator(Favorite favorite){
		List<Error> errors = new ArrayList<>();
		try {
			Favorite favoriteCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(favorite.getId()))) {
				favoriteCheck = favoriteRepository.findById(favorite.getId()).orElse(null);
				if(favoriteCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
			Product productCheck = productRepository.findById(favorite.getProductId()).orElse(null);
			if(productCheck == null || productCheck.getId() == null || productCheck.getStatus() == EStatus.SOLD_OUT)
				errors.add(new Error().builder(ErrorCode.INVALID_PRODUCT));
			
			User userCheck = userRepository.findById(favorite.getUserId()).orElse(null);
			if(userCheck == null || userCheck.getId() == null || userCheck.getStatus() != EStatus.ACTIVE)
				errors.add(new Error().builder(ErrorCode.INVALID_USER));
		}catch(Exception ex) {
			log.error("Error while validating cart data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Favorite addProduct(Favorite favorite) {
		try {
			favorite.prepareEntity();
			favorite = favoriteRepository.save(favorite);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(favorite.getId())))
		        return null;
			
			return favorite;
		}catch(Exception ex) {
			log.error("Error while adding product to favorite", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error removeProduct (String userId, String productId) {
		try {
			Product productCheck = productRepository.findById(productId).orElse(null);
			if(productCheck == null || productCheck.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			User userCheck = userRepository.findById(userId).orElse(null);
			if(userCheck == null || userCheck.getId() == null || userCheck.getStatus() != EStatus.ACTIVE)
				return new Error().builder(ErrorCode.NOT_FOUND);
			Favorite favoriteCheck = favoriteRepository.findByProductUser(userId, productId);
			if(favoriteCheck == null || favoriteCheck.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			favoriteRepository.deleteById(favoriteCheck.getId());
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while removing product from cart", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
}
