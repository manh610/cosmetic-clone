package com.cosmetic.gg.authentication.service;

import java.util.ArrayList;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.authentication.jwt.JwtUtil;
import com.cosmetic.gg.authentication.repository.AuthRepository;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.repository.UserRepository;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class UserDetailsServiceImpl implements UserDetailsService{
	
	@Autowired
    private AuthRepository authRepository;
	
	@Autowired
    private AuthenticationManager authenticationManager;

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		User user = authRepository.findByUsername(username);
        if (user == null) {
            throw new UsernameNotFoundException("User not found with username: " + username);
        }
        return new org.springframework.security.core.userdetails.User(user.getUsername(), user.getPassword(),
                new ArrayList<>());
	}
	
	public Error authenticate(String username, String password) throws Exception{
		try {
			Authentication authentication = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(username, password));
			if(authentication == null) return new Error().builder(ErrorCode.INVALID_CREDENTIALS);
			return new Error().builder(ErrorCode.SUCCESS);
        } catch (DisabledException e) {
        	return new Error().builder(ErrorCode.USER_DISABLED);
        } catch (BadCredentialsException e) {
        	return new Error().builder(ErrorCode.INVALID_CREDENTIALS);
        }
	}
	
	public User getCurrentUser() {
		try {
			Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
			assert authentication != null;
			String username = authentication.getName();
			return authRepository.findByUsername(username);
		}catch(Exception ex) {
			ex.printStackTrace();
			return null;
		}
	}
}
