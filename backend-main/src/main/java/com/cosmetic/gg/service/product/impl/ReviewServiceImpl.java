package com.cosmetic.gg.service.product.impl;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.SecurityUserUtils;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.product.ReviewResponse;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.attribute.ProductImage;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.entity.product.Review;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.attribute.ProductImageRepository;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.repository.product.ReviewRepository;
import com.cosmetic.gg.service.product.ReviewService;

@Service
public class ReviewServiceImpl implements ReviewService{

	private static final Logger log = LoggerFactory.getLogger(ReviewServiceImpl.class);
	
	@Autowired
	private ReviewRepository reviewRepository;
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@Autowired
	private ProductImageRepository imageProductRepository;
	
	@Override
	public List<ReviewResponse> search(String id, Integer star) {
		List<ReviewResponse> result = new ArrayList<>();
		try {
			Product productCheck = productRepository.findById(id).orElse(null);
			if(productCheck == null || productCheck.getId() == null)
				return result;
			List<Object> objects = reviewRepository.search(id, star);
			for(Object rs: objects) {
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ReviewResponse response = new ReviewResponse();
					response.setId((String) objArray[0]);
					response.setUserId((String) objArray[1]);
					response.setProductItemId((String) objArray[2]);
					response.setComment((String) objArray[3]);
					response.setStar((int) objArray[4]);
					response.setCreatedAt(((java.sql.Timestamp) objArray[5]).toLocalDateTime());
					response.setValue((String) objArray[6]);
					
					result.add(response);
				}
			}
		}catch(Exception ex) {
			log.error("Error while searching review", ex.getCause());
		}
		return result;
	}
	
	@Override
	public List<Error> validator(Review review) {
		List<Error> errors = new ArrayList<>();
		try {
			User userCheck = userRepository.findById(review.getUserId()).orElse(null);
			if(userCheck == null || userCheck.getId() == null) {
				errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
			if(userCheck.getStatus() != EStatus.ACTIVE) {
				errors.add(new Error().builder(ErrorCode.INVALID_DATA));
			}
			
			ProductItem productItemCheck = productItemRepository.findById(review.getProductItemId()).orElse(null);
			if(productItemCheck == null || productItemCheck.getId() == null)
				errors.add(new Error().builder(ErrorCode.NOT_FOUND));
		}catch(Exception ex) {
			log.error("Error while validating review data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Review create(Review review) {
		try {
			review.prepareEntity();
			review = reviewRepository.save(review);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(review.getId())))
		        return null;
			
			return review;
		}catch(Exception ex) {
			log.error("Error while creating review", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Review reviewCheck = reviewRepository.findById(id).orElse(null);
			if(reviewCheck == null || reviewCheck.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			
			List<ProductImage> productImage = imageProductRepository.findByReview(id);
			for(ProductImage item: productImage) {
				imageProductRepository.deleteById(item.getId());
			}
			
			reviewRepository.deleteById(id);
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public ErrorCode changeImage(List<MultipartFile> images, String id) {
		try {
			Review reviewCheck = reviewRepository.findById(id).orElse(null);
			if(reviewCheck == null || reviewCheck.getId() == null)
				return ErrorCode.NOT_FOUND;
			
			for(MultipartFile image: images) {
				ProductImage imageProduct = new ProductImage();
				String originalFilename = image.getOriginalFilename();
		        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
		        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
		        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
		        			&& !extension.equals("svg") && !extension.equals("jpeg"))
		        		return ErrorCode.INVALID_IMAGE;
		        }
		        imageProduct.setId("");
				imageProduct.setData(image.getBytes());
				imageProduct.setSize(image.getSize());
				imageProduct.setType(extension);
				String username = SecurityUserUtils.getCurrentUser() == null ?
						"system" : SecurityUserUtils.getCurrentUser().getUsername();
				imageProduct.setUploadedBy(username);
				imageProduct.setUploadedAt(LocalDateTime.now());
				imageProduct.setReviewId(id);
				imageProduct = imageProductRepository.save(imageProduct);
				if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(imageProduct.getId())))
					return ErrorCode.FAILURE;
			}
			
			//kiểm tra nếu save error???
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while uploading image review", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
}
