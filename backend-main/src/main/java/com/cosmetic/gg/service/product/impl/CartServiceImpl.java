package com.cosmetic.gg.service.product.impl;

import java.math.BigDecimal;
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
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.attribute.ProductImage;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.product.Cart;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.attribute.ProductImageRepository;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.product.CartRepository;
import com.cosmetic.gg.repository.product.FavoriteRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.product.CartService;
@Service
public class CartServiceImpl implements CartService{
	
	private static final Logger log = LoggerFactory.getLogger(CartServiceImpl.class);

	@Autowired
	private CartRepository cartRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository; 
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private ProductImageRepository imageProductRepository;
	
	@Autowired
	private FavoriteRepository favoriteRepository;
	
	@Override
	public List<ProductItemResponse> search(String id) {
		List<ProductItemResponse> result = new ArrayList<>();
		try {
			User userCheck = userRepository.findById(id).orElse(null);
			if(userCheck == null || userCheck.getStatus() != EStatus.ACTIVE)
				return result;
			
			List<Cart> carts = cartRepository.getByUser(id);
			for(Cart item: carts) {
				Object rs = productRepository.getProductByProductItem(item.getProductItemId());
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ProductItemResponse productResponse = new ProductItemResponse();
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
					
					BigDecimal total= (BigDecimal) objArray[15];
					productResponse.setTotalQuantity((Integer) total.intValue());
					
					Integer totalFavorite = favoriteRepository.cntProductFavorite((String) objArray[0]);
					productResponse.setTotalFavorite(totalFavorite);
					
					List<String> skinTypePairs = Arrays.asList(((String) objArray[16]).split(","));
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
					
					List<byte[]> images = new ArrayList<>();
					List<ProductImage> productImage = imageProductRepository.findByProductItem(item.getProductItemId());
					for(ProductImage element: productImage) {
						images.add(element.getData());
					}
					productResponse.setImages(images);
					productResponse.setValueDetailId((String) objArray[17]);
					productResponse.setProductItemId((String) objArray[18]);
					productResponse.setValue((String) objArray[19]);
					productResponse.setImportPrice((Float) objArray[20]);
					productResponse.setSellPrice((Float) objArray[21]);
					productResponse.setImportQuantity((Integer) objArray[22]);
					productResponse.setSellQuantity((Integer) objArray[23]);
					
					EStatus status = ((String) objArray[24]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
					productResponse.setValueStatus(status);
					productResponse.setUnit((String) objArray[25]);
					productResponse.setImage((byte[]) objArray[26]);
					productResponse.setQuantity(item.getQuantity());
					productResponse.setCartId(item.getId());
					
					result.add(productResponse);
				}
			}
			return result;
		}catch(Exception ex) {
			log.error("Error while searching product to cart", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public List<Error> validator(Cart cart){
		List<Error> errors = new ArrayList<>();
		try {
			Cart cartCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(cart.getId()))) {
				cartCheck = cartRepository.findById(cart.getId()).orElse(null);
				if(cartCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
			ProductItem productItemCheck = productItemRepository.checkExist(cart.getProductItemId());
			if(productItemCheck == null || productItemCheck.getId() == null)
				errors.add(new Error().builder(ErrorCode.INVALID_PRODUCT));
			
			User userCheck = userRepository.findById(cart.getUserId()).orElse(null);
			if(userCheck == null || userCheck.getId() == null || userCheck.getStatus() != EStatus.ACTIVE)
				errors.add(new Error().builder(ErrorCode.INVALID_USER));
		}catch(Exception ex) {
			log.error("Error while validating cart data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Cart addProduct(Cart cart) {
		try {
			Cart cartCheck = cartRepository.checkExist(cart.getProductItemId(), cart.getUserId());
			if(cartCheck == null || cartCheck.getId() == null) {
				cart.prepareEntity();
				cart = cartRepository.save(cart);
				if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(cart.getId())))
			        return null;
				return cart;
			}else {
				cartCheck.setQuantity(cartCheck.getQuantity() + cart.getQuantity());
				cartCheck = cartRepository.save(cartCheck);
				if (cartCheck == null)
			        return null;
				return cartCheck;
			}
		}catch(Exception ex) {
			log.error("Error while adding product to cart", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Cart updateProduct(String id, Integer quantity) {
		try {
			Cart cartCheck = cartRepository.findById(id).orElse(null);
			if(cartCheck == null || cartCheck.getId() == null)
				return null;
			cartCheck.prepareEntity();
			cartCheck.setQuantity(quantity);
			cartCheck = cartRepository.save(cartCheck);
			if(cartCheck == null)
				return null;
			return cartCheck;
		}catch(Exception ex) {
			log.error("Error while updating product in cart", ex.getCause());
			return null;
		}
	}
	
	@Override
	public Error removeProduct (String id) {
		try {
			Cart cartCheck = cartRepository.findById(id).orElse(null);
			if(cartCheck == null || cartCheck.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			cartRepository.deleteById(cartCheck.getId());
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while removing product from cart", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public Error deleteMulti(List<String> ids) {
		try {
			for(String id: ids) {
				Cart cart = cartRepository.findById(id).orElse(null);
				if(cart == null || cart.getId() == null) 
					return new Error().builder(ErrorCode.NOT_FOUND);
			}
			for(String id: ids) {
				cartRepository.deleteById(id);
			}
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting multiple product from cart", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
}
